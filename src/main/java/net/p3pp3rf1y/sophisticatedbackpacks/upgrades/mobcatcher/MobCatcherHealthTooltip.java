package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.world.inventory.tooltip.TooltipComponent;

public record MobCatcherHealthTooltip(int currentHealth, int maxHealth) implements TooltipComponent {
}
