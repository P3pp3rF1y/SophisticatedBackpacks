package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.common.extensions.IForgeItemStack;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Consumer;

public class ToolSwapperFilterLogic extends FilterLogicBase {
	protected static final Map<String, ToolType> TOOL_TYPES = new HashMap<>(Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(ToolType.class, null, "VALUES")));
	private ItemStack weaponFilter;
	private final Map<ToolType, ItemStack> toolFilters = new TreeMap<>(Comparator.comparing(ToolType::getName));

	public ToolSwapperFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler) {
		super(upgrade, saveHandler, "");
		weaponFilter = NBTHelper.getCompound(upgrade, "weaponFilter").map(ItemStack::read).orElse(ItemStack.EMPTY);
		TOOL_TYPES.forEach((name, tt) -> toolFilters.put(tt, ItemStack.EMPTY));
		loadToolFilters();
	}

	public ItemStack getWeaponFilter() {
		return weaponFilter;
	}

	public ItemStack getToolFilter(ToolType toolType) {
		return toolFilters.get(toolType);
	}

	public Set<ToolType> getToolFilterTypes() {
		return toolFilters.keySet();
	}

	private void loadToolFilters() {
		NBTHelper.getMap(upgrade, "toolFilters", key -> key, (key, nbt) -> ItemStack.read((CompoundNBT) nbt)).ifPresent(savedToolFilters ->
				savedToolFilters.forEach((key, stack) -> {
					if (TOOL_TYPES.containsKey(key)) {
						toolFilters.put(TOOL_TYPES.get(key), stack);
					}
				}));
	}

	public boolean matchesWeaponFilter(ItemStack stack) {
		return !stack.isEmpty() && stack.getMaxStackSize() == 1 && isAllowList() == stackMatchesFilter(stack, weaponFilter);
	}

	public boolean matchesToolFilter(ItemStack stack, ToolType toolType) {
		return !stack.isEmpty() && stack.getMaxStackSize() == 1 && isAllowList() == stackMatchesFilter(stack, toolFilters.getOrDefault(toolType, ItemStack.EMPTY));
	}

	public void setWeaponFilter(ItemStack stack) {
		weaponFilter = stack;
		NBTHelper.setCompoundNBT(upgrade, "weaponFilter", stack.serializeNBT());
		save();
	}

	public void setToolFilter(ToolType toolType, ItemStack stack) {
		toolFilters.put(toolType, stack);
		NBTHelper.setMap(upgrade, "toolFilters", toolFilters, ToolType::getName, IForgeItemStack::serializeNBT);
		save();
	}
}