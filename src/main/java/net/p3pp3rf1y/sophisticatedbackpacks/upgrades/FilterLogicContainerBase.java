package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IFilterSlot;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class FilterLogicContainerBase<T extends FilterLogicBase, S extends Slot> {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";
	private static final String DATA_ADD_TAG_NAME = "addTagName";
	private static final String DATA_REMOVE_TAG_NAME = "removeTagName";
	private static final String DATA_MATCH_ANY_TAG = "matchAnyTag";

	protected final List<S> filterSlots = new ArrayList<>();
	protected final IServerUpdater serverUpdater;
	protected final Supplier<T> filterLogic;

	private final TagSelectionSlot tagSelectionSlot;

	private int selectedTagToAdd = 0;

	private int selectedTagToRemove = 0;

	private final Set<ResourceLocation> tagsToAdd = new TreeSet<>();

	public FilterLogicContainerBase(IServerUpdater serverUpdater, Supplier<T> filterLogic, Consumer<Slot> addSlot) {
		this.serverUpdater = serverUpdater;
		this.filterLogic = filterLogic;

		tagSelectionSlot = new TagSelectionSlot();
		addSlot.accept(tagSelectionSlot);
	}

	public int getSelectedTagToAdd() {
		return selectedTagToAdd;
	}

	public int getSelectedTagToRemove() {
		return selectedTagToRemove;
	}

	public TagSelectionSlot getTagSelectionSlot() {
		return tagSelectionSlot;
	}

	public List<S> getFilterSlots() {
		return filterSlots;
	}

	public Set<ResourceLocation> getTagNames() {
		return filterLogic.get().getTagNames();
	}

	public Set<ResourceLocation> getTagsToAdd() {
		return tagsToAdd;
	}

	public void addSelectedTag() {
		getTagAtIndex(tagsToAdd, selectedTagToAdd).ifPresent(tagName -> {
			addTagName(tagName);
			serverUpdater.sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), DATA_ADD_TAG_NAME, tagName.toString()));
			selectedTagToRemove = 0;
			tagsToAdd.remove(tagName);
			selectedTagToAdd = Math.max(0, selectedTagToAdd - 1);
		});
	}

	private void addTagName(ResourceLocation tagName) {
		filterLogic.get().addTagName(tagName);
	}

	public void removeSelectedTag() {
		getTagAtIndex(getTagNames(), selectedTagToRemove).ifPresent(tagName -> {
			removeSelectedTag(tagName);
			serverUpdater.sendDataToServer(() -> NBTHelper.putString(new CompoundNBT(), DATA_REMOVE_TAG_NAME, tagName.toString()));
			if (tagSelectionSlot.getItem().getItem().getTags().contains(tagName)) {
				tagsToAdd.add(tagName);
			}
			selectedTagToRemove = Math.max(0, selectedTagToRemove - 1);
		});
	}

	private void removeSelectedTag(ResourceLocation tagName) {
		filterLogic.get().removeTagName(tagName);
	}

	public void selectNextTagToRemove() {
		selectedTagToRemove = getNextIndex(getTagNames().size(), selectedTagToRemove);
	}

	private int getNextIndex(int colSize, int selectedIndex) {
		return selectedIndex + 1 >= colSize ? 0 : selectedIndex + 1;
	}

	private int getPreviousIndex(int colSize, int selectedIndex) {
		return selectedIndex == 0 ? colSize - 1 : selectedIndex - 1;
	}

	public void selectPreviousTagToRemove() {
		selectedTagToRemove = getPreviousIndex(getTagNames().size(), selectedTagToRemove);
	}

	public void selectNextTagToAdd() {
		selectedTagToAdd = getNextIndex(tagsToAdd.size(), selectedTagToAdd);
	}

	public void selectPreviousTagToAdd() {
		selectedTagToAdd = getPreviousIndex(tagsToAdd.size(), selectedTagToAdd);
	}

	private Optional<ResourceLocation> getTagAtIndex(Set<ResourceLocation> col, int index) {
		int curIndex = 0;
		for (ResourceLocation tagName : col) {
			if (curIndex == index) {
				return Optional.of(tagName);
			}
			curIndex++;
		}
		return Optional.empty();
	}

	public boolean isAllowList() {
		return filterLogic.get().isAllowList();
	}

	public boolean shouldMatchDurability() {
		return filterLogic.get().shouldMatchDurability();
	}

	public boolean shouldMatchNbt() {
		return filterLogic.get().shouldMatchNbt();
	}

	public PrimaryMatch getPrimaryMatch() {
		return filterLogic.get().getPrimaryMatch();
	}

	public boolean shouldMatchAnyTag() {
		return filterLogic.get().shouldMatchAnyTag();
	}

	public void setAllowList(boolean isAllowList) {
		filterLogic.get().setAllowList(isAllowList);
		serverUpdater.sendBooleanToServer(DATA_IS_ALLOW_LIST, isAllowList);
	}

	public void setMatchDurability(boolean matchDurability) {
		filterLogic.get().setMatchDurability(matchDurability);
		serverUpdater.sendBooleanToServer(DATA_MATCH_DURABILITY, matchDurability);
	}

	public void setMatchNbt(boolean matchNbt) {
		filterLogic.get().setMatchNbt(matchNbt);
		serverUpdater.sendBooleanToServer(DATA_MATCH_NBT, matchNbt);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		filterLogic.get().setPrimaryMatch(primaryMatch);
		serverUpdater.sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_PRIMARY_MATCH, primaryMatch));
	}

	public void setMatchAnyTag(boolean matchAnyTag) {
		filterLogic.get().setMatchAnyTag(matchAnyTag);
		serverUpdater.sendBooleanToServer(DATA_MATCH_ANY_TAG, matchAnyTag);
	}

	public boolean handleMessage(CompoundNBT data) {
		for (String key : data.getAllKeys()) {
			switch (key) {
				case DATA_IS_ALLOW_LIST:
					setAllowList(data.getBoolean(DATA_IS_ALLOW_LIST));
					return true;
				case DATA_MATCH_DURABILITY:
					setMatchDurability(data.getBoolean(DATA_MATCH_DURABILITY));
					return true;
				case DATA_MATCH_NBT:
					setMatchNbt(data.getBoolean(DATA_MATCH_NBT));
					return true;
				case DATA_PRIMARY_MATCH:
					setPrimaryMatch(PrimaryMatch.fromName(data.getString(DATA_PRIMARY_MATCH)));
					return true;
				case DATA_ADD_TAG_NAME:
					addTagName(new ResourceLocation(data.getString(DATA_ADD_TAG_NAME)));
					return true;
				case DATA_REMOVE_TAG_NAME:
					removeSelectedTag(new ResourceLocation(data.getString(DATA_REMOVE_TAG_NAME)));
					return true;
				case DATA_MATCH_ANY_TAG:
					setMatchAnyTag(data.getBoolean(DATA_MATCH_ANY_TAG));
					return true;
				default:
			}
		}
		return false;
	}

	public class TagSelectionSlot extends Slot implements IFilterSlot {
		private ItemStack stack = ItemStack.EMPTY;
		private Runnable onUpdate = () -> {};

		public TagSelectionSlot() {
			super(new Inventory(0), 0, -1, -1);
		}

		public void setOnUpdate(Runnable onUpdate) {
			this.onUpdate = onUpdate;
		}

		@Override
		public boolean mayPlace(ItemStack stack) {
			return stack.isEmpty() || !stack.getItem().getTags().isEmpty();
		}

		@Override
		public boolean mayPickup(PlayerEntity pPlayer) {
			return false;
		}

		@Override
		public ItemStack getItem() {
			return stack;
		}

		@Override
		public int getMaxStackSize() {
			return 1;
		}

		@Override
		public ItemStack remove(int pAmount) {
			stack = ItemStack.EMPTY;
			return stack;
		}

		@Override
		public boolean isSameInventory(Slot other) {
			return false;
		}

		@Override
		public void set(ItemStack stack) {
			this.stack = stack;
			tagsToAdd.clear();
			tagsToAdd.addAll(stack.getItem().getTags());
			getTagNames().forEach(tagsToAdd::remove);
			selectedTagToAdd = 0;
			onUpdate.run();
		}

		@Override
		public void setChanged() {
			//noop
		}
	}
}
