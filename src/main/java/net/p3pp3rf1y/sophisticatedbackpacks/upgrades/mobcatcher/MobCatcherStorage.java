package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;

import java.util.*;

public class MobCatcherStorage {
	public static final String CAPTURED_MOBS_TAG = "capturedMobs";
	private static final String ID_TAG = "id";
	private static final String ENTITY_TYPE_TAG = "entityType";
	private static final String ENTITY_NBT_TAG = "entityNbt";
	private static final String SLOT_TAG = "slot";
	private static final String WIDTH_TAG = "width";
	private static final String HEIGHT_TAG = "height";
	private static final String SLOT_COST_TAG = "slotCost";
	private static final String HOSTILE_TAG = "hostile";
	private static final String DISPLAY_NAME_TAG = "displayName";
	private static final String CURRENT_HEALTH_TAG = "currentHealth";
	private static final String MAX_HEALTH_TAG = "maxHealth";
	private static final double FOOTPRINT_ASPECT_WIDENING = 1.4D;
	private static final double FOOTPRINT_ASPECT_WEIGHT = 10D;
	private static final double FOOTPRINT_OVERFILL_WEIGHT = 0.75D;

	private MobCatcherStorage() {
	}

	public static List<CapturedMob> getCapturedMobs(IBackpackWrapper backpackWrapper) {
		return backpackWrapper.getContentsUuid().map(MobCatcherStorage::getCapturedMobs).orElseGet(List::of);
	}

	public static List<CapturedMob> getCapturedMobs(UUID backpackUuid) {
		CompoundTag contentsNbt = BackpackStorage.get().getOrCreateBackpackContents(backpackUuid);
		List<CapturedMob> capturedMobs = new ArrayList<>();
		for (Tag tag : contentsNbt.getList(CAPTURED_MOBS_TAG).orElse(new ListTag())) {
			capturedMobs.add(deserialize((CompoundTag) tag));
		}
		return capturedMobs;
	}

	public static CompoundTag getCapturedMobsTag(IBackpackWrapper backpackWrapper) {
		CompoundTag ret = new CompoundTag();
		ret.put(CAPTURED_MOBS_TAG, serialize(getCapturedMobs(backpackWrapper)));
		return ret;
	}

	public static void attachSlotBlocking(IBackpackWrapper backpackWrapper) {
		backpackWrapper.attachInventorySlotBlockers();
	}

	public static boolean isSlotBlocked(IBackpackWrapper backpackWrapper, int slot) {
		int columns = getColumns(backpackWrapper);
		return getCapturedMobs(backpackWrapper).stream().anyMatch(mob -> mob.occupiesSlot(slot, columns));
	}

	public static boolean canFitBasicTier(IBackpackWrapper backpackWrapper, int maxSlotCost) {
		return getCapturedMobs(backpackWrapper).stream().allMatch(mob -> !mob.hostile() && mob.slotCost() <= maxSlotCost);
	}

	public static boolean isInventoryLayoutSlotHandled(IBackpackWrapper backpackWrapper, int slot, int columns) {
		for (CapturedMob capturedMob : getCapturedMobs(backpackWrapper)) {
			if (capturedMob.slot() == slot || capturedMob.occupiesSlot(slot, columns)) {
				return true;
			}
		}
		return false;
	}

	public static Optional<InventoryLayoutPart> getInventoryLayoutPart(IBackpackWrapper backpackWrapper, int slot, int columns, int targetColumns) {
		InventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();
		return getCapturedMobs(backpackWrapper).stream()
			.filter(capturedMob -> capturedMob.slot() == slot)
			.findFirst()
			.map(capturedMob -> new InventoryLayoutPart(getLayoutPartId(capturedMob), getTargetSlot(capturedMob.slot(), columns, targetColumns), capturedMob.width(), capturedMob.height(), getOccupiedSlots(capturedMob, columns, inventoryHandler.getSlots())));
	}

	public static void applyInventoryLayout(IBackpackWrapper backpackWrapper, InventoryLayoutFitResult fitResult, int columns) {
		if (!fitResult.fits()) {
			return;
		}

		boolean changed = false;
		List<CapturedMob> capturedMobs = new ArrayList<>();
		for (CapturedMob capturedMob : getCapturedMobs(backpackWrapper)) {
			int fittedSlot = fitResult.fittedSlots().getOrDefault(getLayoutPartId(capturedMob), capturedMob.slot());
			if (fittedSlot != capturedMob.slot()) {
				changed = true;
				capturedMobs.add(new CapturedMob(capturedMob.id(), capturedMob.entityType(), capturedMob.entityNbt(), fittedSlot, capturedMob.width(), capturedMob.height(), capturedMob.slotCost(), capturedMob.hostile(), capturedMob.displayName(), capturedMob.currentHealth(), capturedMob.maxHealth()));
			} else {
				capturedMobs.add(capturedMob);
			}
		}

		if (changed) {
			saveCapturedMobs(backpackWrapper, capturedMobs);
		} else {
			attachSlotBlocking(backpackWrapper);
		}
	}

	public static Optional<Integer> findEmptyRectangle(IBackpackWrapper backpackWrapper, CapturedMobFootprint footprint) {
		InventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();
		int columns = getColumns(backpackWrapper);
		int rows = (int) Math.ceil((double) inventoryHandler.getSlots() / columns);
		List<CapturedMob> capturedMobs = getCapturedMobs(backpackWrapper);
		for (int y = 0; y <= rows - footprint.height(); y++) {
			for (int x = 0; x <= columns - footprint.width(); x++) {
				int slot = y * columns + x;
				if (isRectangleEmpty(slot, footprint, columns, inventoryHandler, capturedMobs)) {
					return Optional.of(slot);
				}
			}
		}
		return Optional.empty();
	}

	private static boolean isRectangleEmpty(int slot, CapturedMobFootprint footprint, int columns, InventoryHandler inventoryHandler, List<CapturedMob> capturedMobs) {
		for (int y = 0; y < footprint.height(); y++) {
			for (int x = 0; x < footprint.width(); x++) {
				int checkedSlot = slot + y * columns + x;
				if (checkedSlot >= inventoryHandler.getSlots() || !inventoryHandler.isSlotAccessible(checkedSlot) || !inventoryHandler.getStackInSlot(checkedSlot).isEmpty()) {
					return false;
				}
				for (CapturedMob capturedMob : capturedMobs) {
					if (capturedMob.occupiesSlot(checkedSlot, columns)) {
						return false;
					}
				}
			}
		}
		return true;
	}

	public static void addCapturedMob(IBackpackWrapper backpackWrapper, CapturedMob capturedMob) {
		updateCapturedMobs(backpackWrapper, capturedMobs -> capturedMobs.add(capturedMob));
	}

	public static void removeCapturedMob(IBackpackWrapper backpackWrapper, UUID capturedMobId) {
		List<CapturedMob> capturedMobs = new ArrayList<>(getCapturedMobs(backpackWrapper));
		Optional<CapturedMob> removed = capturedMobs.stream().filter(mob -> mob.id().equals(capturedMobId)).findFirst();
		removed.ifPresent(mob -> {
			capturedMobs.remove(mob);
			saveCapturedMobs(backpackWrapper, capturedMobs);
		});
	}

	public static Optional<CapturedMob> getCapturedMob(IBackpackWrapper backpackWrapper, UUID capturedMobId) {
		return getCapturedMobs(backpackWrapper).stream().filter(mob -> mob.id().equals(capturedMobId)).findFirst();
	}

	private static void updateCapturedMobs(IBackpackWrapper backpackWrapper, java.util.function.Consumer<List<CapturedMob>> updater) {
		List<CapturedMob> capturedMobs = new ArrayList<>(getCapturedMobs(backpackWrapper));
		updater.accept(capturedMobs);
		saveCapturedMobs(backpackWrapper, capturedMobs);
	}

	private static void saveCapturedMobs(IBackpackWrapper backpackWrapper, List<CapturedMob> capturedMobs) {
		Optional<UUID> contentsUuid = backpackWrapper.getContentsUuid();
		if (contentsUuid.isEmpty()) {
			return;
		}
		CompoundTag contentsNbt = BackpackStorage.get().getOrCreateBackpackContents(contentsUuid.get());
		if (capturedMobs.isEmpty()) {
			contentsNbt.remove(CAPTURED_MOBS_TAG);
		} else {
			contentsNbt.put(CAPTURED_MOBS_TAG, serialize(capturedMobs));
		}
		BackpackStorage.get().setDirty();
		attachSlotBlocking(backpackWrapper);
	}

	private static ListTag serialize(List<CapturedMob> capturedMobs) {
		ListTag listTag = new ListTag();
		capturedMobs.stream().sorted(Comparator.comparingInt(CapturedMob::slot)).map(MobCatcherStorage::serialize).forEach(listTag::add);
		return listTag;
	}

	private static CompoundTag serialize(CapturedMob capturedMob) {
		CompoundTag tag = new CompoundTag();
		tag.putString(ID_TAG, capturedMob.id().toString());
		tag.putString(ENTITY_TYPE_TAG, capturedMob.entityType().toString());
		tag.put(ENTITY_NBT_TAG, capturedMob.entityNbt().copy());
		tag.putInt(SLOT_TAG, capturedMob.slot());
		tag.putInt(WIDTH_TAG, capturedMob.width());
		tag.putInt(HEIGHT_TAG, capturedMob.height());
		tag.putInt(SLOT_COST_TAG, capturedMob.slotCost());
		tag.putBoolean(HOSTILE_TAG, capturedMob.hostile());
		tag.putString(DISPLAY_NAME_TAG, capturedMob.displayName());
		tag.putInt(CURRENT_HEALTH_TAG, capturedMob.currentHealth());
		tag.putInt(MAX_HEALTH_TAG, capturedMob.maxHealth());
		return tag;
	}

	private static CapturedMob deserialize(CompoundTag tag) {
		int maxHealth = tag.getIntOr(MAX_HEALTH_TAG, 1);
		int currentHealth = tag.getIntOr(CURRENT_HEALTH_TAG, maxHealth);
		return new CapturedMob(
			UUID.fromString(tag.getString(ID_TAG).orElse(UUID.randomUUID().toString())),
			ResourceLocation.parse(tag.getString(ENTITY_TYPE_TAG).orElse("minecraft:pig")),
			tag.getCompound(ENTITY_NBT_TAG).orElse(new CompoundTag()),
			tag.getIntOr(SLOT_TAG, 0),
			tag.getIntOr(WIDTH_TAG, 1),
			tag.getIntOr(HEIGHT_TAG, 1),
			tag.getIntOr(SLOT_COST_TAG, 1),
			tag.getBooleanOr(HOSTILE_TAG, false),
			tag.getString(DISPLAY_NAME_TAG).orElse(""),
			Math.max(0, currentHealth),
			Math.max(1, maxHealth)
		);
	}

	public static CapturedMobFootprint getFootprint(LivingEntity entity, int slotCost) {
		float width = Math.max(entity.getBbWidth(), 0.25F);
		float height = Math.max(entity.getBbHeight(), 0.25F);
		double targetAspect = width / height * FOOTPRINT_ASPECT_WIDENING;
		CapturedMobFootprint best = new CapturedMobFootprint(1, Math.max(1, slotCost));
		double bestScore = Double.MAX_VALUE;
		double bestAspectError = Double.MAX_VALUE;
		int bestOverfill = Integer.MAX_VALUE;
		for (int w = 1; w <= Math.max(1, slotCost); w++) {
			for (int h = 1; h <= Math.max(1, slotCost); h++) {
				int area = w * h;
				if (area < slotCost) {
					continue;
				}
				double aspect = (double) w / h;
				double aspectError = Math.abs(Math.log(aspect / targetAspect));
				int overfill = area - slotCost;
				double score = aspectError * FOOTPRINT_ASPECT_WEIGHT + overfill * FOOTPRINT_OVERFILL_WEIGHT;
				if (isBetterFootprint(score, aspectError, overfill, w, h, bestScore, bestAspectError, bestOverfill, best)) {
					bestScore = score;
					bestAspectError = aspectError;
					bestOverfill = overfill;
					best = new CapturedMobFootprint(w, h);
				}
			}
		}
		return best;
	}

	private static boolean isBetterFootprint(double score, double aspectError, int overfill, int width, int height, double bestScore, double bestAspectError, int bestOverfill, CapturedMobFootprint best) {
		if (score < bestScore - 0.001D) {
			return true;
		}
		if (score > bestScore + 0.001D) {
			return false;
		}
		if (aspectError < bestAspectError - 0.001D) {
			return true;
		}
		if (aspectError > bestAspectError + 0.001D) {
			return false;
		}
		if (width != best.width()) {
			return width > best.width();
		}
		if (height != best.height()) {
			return height < best.height();
		}
		return overfill < bestOverfill;
	}

	public static int getColumns(IBackpackWrapper backpackWrapper) {
		int slots = backpackWrapper.getInventoryHandler().getSlots() + backpackWrapper.getColumnsTaken() * backpackWrapper.getNumberOfSlotRows();
		return (slots <= 81 ? 9 : 12) - backpackWrapper.getColumnsTaken();
	}

	private static String getLayoutPartId(CapturedMob capturedMob) {
		return "mob:" + capturedMob.id();
	}

	private static Set<Integer> getOccupiedSlots(CapturedMob capturedMob, int columns, int inventorySlots) {
		Set<Integer> occupiedSlots = new HashSet<>();
		for (int y = 0; y < capturedMob.height(); y++) {
			for (int x = 0; x < capturedMob.width(); x++) {
				int slot = capturedMob.slot() + y * columns + x;
				if (slot < inventorySlots) {
					occupiedSlots.add(slot);
				}
			}
		}
		return occupiedSlots;
	}

	private static int getTargetSlot(int slot, int columns, int targetColumns) {
		return slot / columns * targetColumns + slot % columns;
	}

	public static Optional<net.minecraft.world.entity.EntityType<?>> getEntityType(CapturedMob capturedMob) {
		return BuiltInRegistries.ENTITY_TYPE.getOptional(capturedMob.entityType());
	}

}
